% ============================================================================
% CONSTRAINT STORY: gbff_funding_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gbff_funding_mechanism, []).

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
 *   constraint_id: gbff_funding_mechanism
 *   human_readable: Global Biodiversity Framework Fund (GBFF) Funding Mechanism
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The Global Biodiversity Framework Fund (GBFF) is a multilateral
 *   coordination mechanism designed to mobilize capital from wealthy nations
 *   toward biodiversity conservation in developing nations. From one
 *   perspective, it solves a genuine collective action problem: global
 *   biodiversity loss requires cross-border financing, and wealthier nations
 *   benefit from ecosystem services provided by developing nations'
 *   conservation efforts. From another perspective, GBFF embodies asymmetric
 *   extraction: wealthy donor nations use conditionality frameworks to
 *   enforce conservation priorities aligned with their interests (carbon
 *   sequestration, species of commercial value), while developing nations
 *   bear implementation costs and sovereignty constraints. Indigenous
 *   communities experience GBFF as a snare: access to conservation funding is
 *   conditioned on abandoning traditional land management practices and
 *   accepting external territorial restrictions. The constraint exhibits the
 *   signature of a Tangled Rope — genuine coordination function (funding
 *   mobilization) combined with asymmetric extraction (conditionality
 *   asymmetry, sovereignty imposition, indigenous exclusion). The theater
 *   ratio (0.68) reflects growing compliance reporting burden detached from
 *   conservation outcomes: the GBFF administrative apparatus generates
 *   extensive monitoring frameworks, fiduciary safeguards, and reporting
 *   requirements that are increasingly performative rather than functional.
 *
 * KEY AGENTS:
 *   - Wealthy Donor Nations: Institutional/arbitrage — primary beneficiaries, control allocation priorities through conditionality frameworks, experience minimal extraction
 *   - Developing Nations (Fund Recipients): Moderate/constrained — asymmetrically constrained by conditionalities, dependent on fund availability, bear implementation costs and sovereignty constraints
 *   - International Conservation NGOs: Institutional/arbitrage — secondary beneficiaries, gain operational funding, experience low extraction relative to coordination benefits
 *   - Indigenous Communities: Powerless/trapped — excluded from conservation strategy decisions, displaced by protected area designations, experience maximum extraction with no exit options
 *   - GBFF Administrative Apparatus: Institutional/arbitrage — maintains large overhead with growing theater ratio; original coordination function persists but increasingly subordinated to compliance metrics
 *   - Analytical Observer: Analytical/analytical — observes genuine coordination function (capital mobilization) combined with asymmetric extraction (conditionality control, sovereignty constraint, indigenous displacement)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gbff_funding_mechanism, 0.58).
domain_priors:suppression_score(gbff_funding_mechanism, 0.62).
domain_priors:theater_ratio(gbff_funding_mechanism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gbff_funding_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(gbff_funding_mechanism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gbff_funding_mechanism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gbff_funding_mechanism, tangled_rope).
narrative_ontology:human_readable(gbff_funding_mechanism, "Global Biodiversity Framework Fund (GBFF) Funding Mechanism").
narrative_ontology:topic_domain(gbff_funding_mechanism, "geopolitical/economic").

domain_priors:requires_active_enforcement(gbff_funding_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gbff_funding_mechanism, wealthy_donor_nations).
narrative_ontology:constraint_beneficiary(gbff_funding_mechanism, international_conservation_ngos).
narrative_ontology:constraint_beneficiary(gbff_funding_mechanism, gbff_administrative_apparatus).
narrative_ontology:constraint_victim(gbff_funding_mechanism, biodiversity_conservation_outcomes).
narrative_ontology:constraint_victim(gbff_funding_mechanism, developing_nations_sovereignty).
narrative_ontology:constraint_victim(gbff_funding_mechanism, indigenous_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS COMMUNITIES (SNARE) — Trapped by funding conditionalities that exclude traditional land management practices and impose externally-defined conservation targets. No exit from dependence on GBFF allocations for ecosystem protection. Bears full cost of enforcement through displacement, land restrictions, and loss of resource access. Maximum experienced extraction with no alternative pathways.
constraint_indexing:constraint_classification(gbff_funding_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATIONS (TANGLED ROPE) — Constrained by limited domestic conservation financing but also coordinate conservation goals through GBFF mechanisms. Experience asymmetric extraction: must adopt biodiversity frameworks aligned with donor priorities while bearing implementation costs. Partial agency through negotiation, but constrained by debt dynamics and alternative funding scarcity. Mixed coordination benefit (funding availability) with asymmetric costs (sovereignty over conservation strategy).
constraint_indexing:constraint_classification(gbff_funding_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WEALTHY DONOR NATIONS (ROPE) — Experience GBFF as pure coordination mechanism for meeting Paris Agreement biodiversity commitments with minimal domestic disruption. Fund allocation to developing nations reduces pressure for domestic industrial regulation. Net beneficiary through reputational gains and carbon credit arbitrage potential. High exit optionality — can adjust contribution levels without constraint.
constraint_indexing:constraint_classification(gbff_funding_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL CONSERVATION NGOs (ROPE) — Benefit from GBFF funding streams for implementation operations. Experience primarily as coordination mechanism enabling conservation projects in developing nations. High exit optionality through diversified funding sources (bilateral grants, foundations, private donors). Experience minimal extraction relative to coordination benefits gained.
constraint_indexing:constraint_classification(gbff_funding_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GBFF ADMINISTRATIVE APPARATUS (PITON) — Maintains large institutional overhead (fiduciary committees, reporting requirements, monitoring-and-evaluation frameworks) that is substantially performative. Original coordination function (mobilizing capital for biodiversity) persists, but growing theater ratio indicates metrics fixation over conservation outcomes. Theater derives from compliance reporting burden detached from actual biodiversity impact. Institution persists through bureaucratic inertia and donor lock-in rather than functional necessity.
constraint_indexing:constraint_classification(gbff_funding_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, GBFF exhibits genuine coordination function (mobilizing conservation capital across borders) AND asymmetric extraction (donor nations control priority-setting, developing nations bear implementation and sovereignty costs). The constraint combines real coordination benefit (funding available) with structural asymmetry (conditionality). Not a pure snare because coordination genuinely occurs; not pure rope because extraction is genuine and asymmetric. Tangled rope from systemic view.
constraint_indexing:constraint_classification(gbff_funding_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gbff_funding_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gbff_funding_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gbff_funding_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gbff_funding_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gbff_funding_mechanism, TR),
    TR >= 0.70.

:- end_tests(gbff_funding_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. GBFF exhibits meaningful extraction: wealthy donor nations capture agenda-setting power through conditionality frameworks, developing nations bear asymmetric implementation costs relative to funding received, and indigenous communities bear displacement costs with minimal compensation. The initial extractiveness (0.35) reflects legitimate capital mobilization function; it increases to 0.58 as the conditionality apparatus expands and donor nations leverage compliance reporting to reinforce priority control. Not maximal (≥0.70) because the fund does allocate resources and some coordination genuinely occurs — this is not pure rent extraction. Suppression (0.62): High. Developing nations face suppressed exit options: limited alternative funding for conservation creates financial dependence on GBFF; conditionalities restrict policy autonomy; international pressure (Paris Agreement alignment) makes non-participation costly. Indigenous communities face total suppression: no recognized land tenure, no consultation authority, no alternative funding for traditional resource management. Wealthy donor nations face minimal suppression (high arbitrage optionality). Theater ratio (0.68): Moderately high. GBFF compliance frameworks have grown increasingly performative: monitoring metrics (hectares protected, species monitored) substitute for actual biodiversity outcome measurement; fiduciary safeguards create administrative theater; reporting burden rises faster than conservation efficacy. Theater is not dominant (≥0.70 would indicate Piton) because the fund does allocate real capital, but theater is substantial enough to indicate institutional drift toward compliance theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival disagreement. Indigenous communities see a Snare (trapped, no exit, pure extraction, displacement). Developing nations see a Tangled Rope (constrained exit, mixed coordination and extraction through conditionality asymmetry). Wealthy donor nations and international NGOs see a Rope (coordination mechanism, high arbitrage optionality, net benefit). The GBFF apparatus sees a Piton (performative compliance theater). The analytical observer sees a Tangled Rope (genuine coordination function + asymmetric extraction of conditionality control). The gap arises from differential power positions: those who control conditionalities experience coordination; those subordinate to conditionalities experience extraction. This gap is structurally unavoidable in the current GBFF architecture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the structural relationship to the extraction mechanism (conditionality control) and the funding flow: Wealthy donor nations are beneficiaries with arbitrage exit (low d, negative effective extraction experienced). Developing nations are partial victims with constrained exit (moderate d due to financial dependence reducing exit optionality). Indigenous communities are full victims with trapped exit (high d, maximum experienced extraction). The GBFF apparatus is an institutional beneficiary (low d from control authority, but degraded by theater ratio indicating functional atrophy). The analytical observer notes that the extraction mechanism is asymmetric conditionality control, not funding scarcity — this is a political economy of aid architecture, not a mere coordination problem. The derived directionality values reflect these structural asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in GBFF classification arises from the genuine coordination function (mobilizing capital across borders for global public good) masking asymmetric extraction mechanisms (conditionality control, sovereignty subordination, indigenous displacement). Resolution requires disaggregating the coordination benefit (real: capital mobilization that would not occur without GBFF architecture) from the extraction mechanism (real: conditionality frameworks that ensure wealthy nations define conservation priorities and developing nations bear disproportionate costs). From a beneficiary perspective (wealthy donors), GBFF is primarily coordination (Rope-type classification). From a victim perspective (developing nations, indigenous communities), GBFF is primarily extraction (Snare or Tangled Rope). The systemic (analytical) perspective sees both simultaneously: the constraint is Tangled Rope — it solves a real collective action problem (biodiversity capital mobilization) while enabling asymmetric power extraction (conditionality control). Mandatrophy is not resolved — the classification remains genuinely ambiguous depending on whether one foregrounds the coordination function or the extraction mechanism. The constraint's legitimacy depends on demonstrating that conditionality asymmetry is necessary for coordination rather than exploiting the need for coordination to impose donor control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_extraction_boundary,
    'At what point do biodiversity framework conditionalities cross from reasonable coordination requirement to extractive sovereignty violation?',
    'Comparative analysis of conservation outcomes in high-conditionality vs low-conditionality funding arrangements; measurement of actual biodiversity improvement vs compliance theater costs',
    'If high conditionality yields proportionally better outcomes: coordination function dominates (Rope likely). If outcomes plateau while compliance costs rise: extraction dominates (Snare likely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_extraction_boundary, empirical, 'Threshold at which conditionalities become extractive rather than coordinative').

omega_variable(
    indigenous_inclusion_authenticity,
    'Are indigenous community consultation mechanisms in GBFF funding allocation authentic co-governance or performative legitimation?',
    'Analysis of indigenous influence on actual fund allocation decisions; correlation between indigenous priorities and funding patterns; tracking of cases where indigenous input overrode donor preferences vs deferred to donor framework',
    'If authentic: indigenous perspective shifts from Snare to Tangled Rope (some agency). If performative: indigenous perspective remains Snare (consultation theater masks top-down control).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_inclusion_authenticity, empirical, 'Whether indigenous participation in GBFF governance is authentic or performative').

omega_variable(
    conservation_outcome_measurement_validity,
    'Do GBFF monitoring metrics (hectares protected, species monitored) accurately measure biodiversity conservation or substitute symbolic compliance for functional outcomes?',
    'Longitudinal tracking of GBFF-funded conservation sites; independent ecological assessment vs GBFF-reported metrics; measurement of long-term species persistence and ecosystem health in protected areas',
    'If metrics track real outcomes: fund achieves coordination function and theater is minimal (Rope or Tangled Rope). If metrics decouple from outcomes: theater ratio is underestimated and Piton diagnosis is too favorable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conservation_outcome_measurement_validity, empirical, 'Validity of GBFF outcome metrics in tracking actual conservation').

omega_variable(
    donor_conditionality_consistency,
    'Do wealthy donor nations apply GBFF conditionalities uniformly across developing nation recipients, or do they adjust requirements based on geopolitical alignment?',
    'Comparative review of funding conditionalities across developing nations stratified by donor alignment; analysis of negotiation outcomes where recipient nations rejected conditionalities',
    'If uniform: conditionalities are coordination mechanism (Rope likely). If variable by alignment: conditionalities are extraction and control mechanism (Snare or Tangled Rope for recipients).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(donor_conditionality_consistency, empirical, 'Consistency of donor conditionalities across recipient nations').

omega_variable(
    alternative_funding_availability,
    'Do developing nations have viable alternative funding pathways for conservation if GBFF allocations are reduced or conditionalities increase?',
    'Mapping of non-GBFF biodiversity financing sources available to developing nations; analysis of funding constraints for conservation absent GBFF; longitudinal tracking of bilateral vs multilateral conservation funding patterns',
    'If alternatives exist: recipients have meaningful exit optionality (constraints or mobile, not trapped — classification shifts). If no alternatives: recipients are trapped (supports Snare diagnosis for developing nations and indigenous communities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_availability, empirical, 'Availability of alternative conservation financing pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gbff_funding_mechanism, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gbff_tr_t0, gbff_funding_mechanism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gbff_tr_t3, gbff_funding_mechanism, theater_ratio, 3, 0.55).
narrative_ontology:measurement(gbff_tr_t6, gbff_funding_mechanism, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(gbff_be_t0, gbff_funding_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gbff_be_t3, gbff_funding_mechanism, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(gbff_be_t6, gbff_funding_mechanism, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gbff_funding_mechanism, resource_allocation).
narrative_ontology:affects_constraint(gbff_funding_mechanism, indigenous_land_rights_constraint).
narrative_ontology:affects_constraint(gbff_funding_mechanism, climate_finance_adequacy).
narrative_ontology:affects_constraint(gbff_funding_mechanism, sovereign_development_capacity).

% DUAL FORMULATION NOTE:
% GBFF Funding Mechanism represents the multilateral apparatus for coordinating biodiversity financing. Downstream constraints include indigenous land rights (which GBFF structures restrict), climate finance adequacy (GBFF contributes but is insufficient), and sovereign development capacity (constrained by GBFF conditionalities). The extraction mechanism in GBFF (conditionality asymmetry) is distinct from but causally linked to downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gbff_funding_mechanism, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
