% ============================================================================
% CONSTRAINT STORY: international_ngo_operational_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_ngo_operational_reduction, []).

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
 *   constraint_id: international_ngo_operational_reduction
 *   human_readable: International NGO Operational Reduction Through Regulatory Constraints
 *   domain: political_economy/civil_society
 *
 * SUMMARY:
 *   International NGOs operating in restrictive environments face a
 *   structural constraint that operates through regulatory accumulation: visa
 *   denials, movement permits, banking access restrictions, local hiring
 *   bans, facility seizures, funding cuts, and reporting burdens. This
 *   constraint exhibits the full range of DR classifications depending on the
 *   observer's structural position. Field workers see it as a snare — they
 *   face maximum suppression (trapped by family, community, professional
 *   identity) with minimal exit options and escalating extraction of their
 *   labor capacity under degraded conditions. Vulnerable beneficiary
 *   populations see it as a snare — they are trapped by dependency on
 *   services (medical, educational, protective) that only NGOs provide, and
 *   face extraction through service reduction or withdrawal. The host state
 *   government sees it as a rope — a coordination mechanism for managing
 *   civil society, controlling service delivery narratives, and channeling
 *   aid flows. International headquarters see it as a piton — they maintain
 *   operations through performative compliance theater (local CEOs, strategic
 *   communication, regulatory docility) while actual service provision
 *   capacity has atrophied. The constraint accumulates over time:
 *   extractiveness rises from 0.35 to 0.62 across a decade as regulations
 *   layer upon each other; theater ratio rises from 0.30 to 0.55 as
 *   organizations spend more effort on compliance performance than actual
 *   impact. The analytical observer risks seeing this as a natural law of
 *   sovereignty (mountain) — states inherently control foreign organizations
 *   — but this naturalizes a contingent policy choice. Comparative analysis
 *   reveals that NGO restrictions correlate not with universal sovereignty
 *   claims but with specific threatened elites, making the mountain
 *   classification a false summit that obscures the extractive mechanism.
 *
 * KEY AGENTS:
 *   - Field Workers: Primary victims (powerless/trapped) — face visa denials, movement restrictions, banking exclusion, local embedding creates identity lock preventing exit despite nominal freedom to leave
 *   - Vulnerable Beneficiary Populations: Primary victims (powerless/trapped) — dependent on NGO services with no alternative providers; face service reduction as operational constraints accumulate
 *   - Host State Government: Primary beneficiary (institutional/arbitrage) — controls service delivery narrative, channels aid flows, extracts implicit taxation on NGO operations, has full arbitrage (can escalate or reduce restrictions)
 *   - NGO Field Operations: Secondary victim (organized/constrained) — operationally degraded by restrictions; high-cost exit (abandons beneficiary relationships, organizational mission)
 *   - Domestic NGO Partners: Mixed (organized/constrained) — constrained by restrictions but enabled as preferred intermediaries; experience tangled rope (coordination + extraction)
 *   - International NGO Headquarters: Secondary actor (institutional/constrained) — maintains operations through performative compliance; actual capacity has atrophied to piton status
 *   - International Donor Community: Secondary actor (powerful/mobile) — experience tangled rope (reduced control visibility but reduced fungibility scrutiny); mobile exit but constrained by political relationships
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing extraction as sovereignty law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_ngo_operational_reduction, 0.62).
domain_priors:suppression_score(international_ngo_operational_reduction, 0.68).
domain_priors:theater_ratio(international_ngo_operational_reduction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_ngo_operational_reduction, extractiveness, 0.62).
narrative_ontology:constraint_metric(international_ngo_operational_reduction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(international_ngo_operational_reduction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_ngo_operational_reduction, snare).
narrative_ontology:human_readable(international_ngo_operational_reduction, "International NGO Operational Reduction Through Regulatory Constraints").
narrative_ontology:topic_domain(international_ngo_operational_reduction, "political_economy/civil_society").

domain_priors:requires_active_enforcement(international_ngo_operational_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_ngo_operational_reduction, host_state_government).
narrative_ontology:constraint_victim(international_ngo_operational_reduction, ngo_field_workers).
narrative_ontology:constraint_victim(international_ngo_operational_reduction, vulnerable_beneficiary_populations).
narrative_ontology:constraint_victim(international_ngo_operational_reduction, ngo_organizational_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD WORKER (SNARE) — NGO staff in-country face accumulating restrictions: visa denials, movement permits, banking access, local hiring bans, facility seizure. Exit is nominal (can leave country) but costs are catastrophic (abandons family, employment, identity as aid provider, local community relationships). Trapped by family dependency, local embeddedness, and identity fusion with the aid role. Experiences maximum extraction — the constraint extracts their labor capacity, their geographic immobility, and their willingness to operate under degraded conditions.
constraint_indexing:constraint_classification(international_ngo_operational_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VULNERABLE POPULATION (SNARE) — Communities dependent on NGO services (healthcare, education, food security, protection) face service reduction or withdrawal. No exit options — cannot relocate to alternative service providers (none exist); cannot exit dependency (medical, nutritional, safety needs are non-negotiable). Suppression is structural: geographic isolation, poverty, legal status. The constraint extracts their access to essential services; the state captures the implicit tax on their survival.
constraint_indexing:constraint_classification(international_ngo_operational_reduction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC NGO PARTNER (TANGLED ROPE) — Local organizations receive both constraints and enabling mechanisms: restrictions on international funding, movement, and hiring limit their autonomy; simultaneously, they become intermediaries preferred by the state (lower political threat than international NGOs), gaining access to state resources and legitimacy. They coordinate service delivery with the state while experiencing extraction through dependence on state goodwill. Significant agency but high-cost exit.
constraint_indexing:constraint_classification(international_ngo_operational_reduction, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HOST STATE GOVERNMENT (ROPE) — Experiences the constraint as coordination: managing NGO presence, controlling service delivery narratives, channeling aid flows. Regulation appears as coordination mechanism (managing civil society, directing resources). The state has arbitrage: can escalate restrictions, reduce them, shift them to different sectors. Net beneficiary — extracts political control, resource allocation authority, and implicit taxation on aid flows.
constraint_indexing:constraint_classification(international_ngo_operational_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL NGO HEADQUARTERS (PITON) — Executive leadership maintains operations despite mounting restrictions through performative compliance theater: hiring local CEOs to create distance from international control, filing reports that minimize political sensitivity, maintaining public relations narrative of 'partnership' with government while operations degrade. The organization has atrophied its actual coordination function (service provision is diminished, risk management is reduced to regulatory compliance theater) but persists due to institutional inertia, donor expectations, and sunk organizational identity. Theater ratio high because strategic communication now consumes more organizational effort than actual field impact.
constraint_indexing:constraint_classification(international_ngo_operational_reduction, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL DONOR COMMUNITY (TANGLED ROPE) — Donors (bilateral aid agencies, foundations, multilaterals) experience the constraint as a coordination problem with embedded extraction. The state's restrictions reduce donor control and visibility (coordination cost); simultaneously, donors benefit from reduced scrutiny of aid effectiveness and fungibility concerns. Donors have mobile exit (can redirect funding) but constrained by political relationships and institutional mandates. They coordinate funding flows while experiencing extraction through reduced leverage. Strong perspectival asymmetry: donors have mobility; field workers do not.
constraint_indexing:constraint_classification(international_ngo_operational_reduction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SOVEREIGNTY VIEW (MOUNTAIN) — From a civilizational perspective, state control over foreign organizations within its borders is an immutable feature of sovereignty itself. NGO autonomy and state control are in logical opposition — any NGO operating in a state cannot fully escape state authority. The constraint appears as a natural law of political organization. However, this risks naturalizing what is actually a contingent institutional arrangement (voluntary vs coercive state-NGO relations), and the structural data contradicts the mountain classification. The false summit reveals that 'sovereignty requires extraction' is a preference, not a law.
constraint_indexing:constraint_classification(international_ngo_operational_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_ngo_operational_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_ngo_operational_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_ngo_operational_reduction, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_ngo_operational_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_ngo_operational_reduction, TR),
    TR >= 0.70.

:- end_tests(international_ngo_operational_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts from multiple agents simultaneously — field workers lose autonomy and operate under degraded conditions; vulnerable populations lose service access; organizations lose operational capacity. The extraction accelerates over time (rises from 0.35 to 0.62) as regulations compound. The state captures political control, resource allocation authority, and implicit taxation on aid flows. This is substantial extraction but not maximal because some organizations maintain operations and some services continue, and because the extraction is mediated through regulation rather than explicit confiscation. Suppression (0.68): High. Field workers face cumulative barriers: visa denials, movement permits, banking access restrictions, local hiring bans. But suppression is not total — some NGOs operate in diaspora, remote capacity exists, advocacy networks function internationally. The suppression is sufficient to prevent organized exit but insufficient to eliminate all alternatives. Beneficiary populations face maximal suppression (geographic isolation, poverty, non-negotiable needs). Theater ratio (0.55): Moderate-high. International NGO headquarters increasingly allocate resources to compliance performance (local CEO appointments, strategic communication, regulatory docility reporting) rather than impact maximization. However, the theater is not dominant — significant resources still flow to actual service provision, and the constraint's extraction mechanism is not primarily performative but regulatory. The rising theater ratio (from 0.30 to 0.55) reflects organizational degradation toward piton status.
 *
 * PERSPECTIVAL GAP:
 *   The most salient perspectival gap is between field workers/beneficiaries (snare) and the host state (rope). Both are perceiving the same regulatory constraint, but one group experiences it as deliberate extraction designed to reduce their agency and services, while the other experiences it as rational governance and coordination. This gap is not resolvable by additional data — it reflects genuine structural difference in how the same mechanism is experienced. A secondary gap appears between international headquarters (piton) and field workers (snare). Headquarters have moved toward performative compliance and organizational degradation, while field workers continue to experience escalating extraction. The headquarters' piton perspective reflects the organization's adaptive response to suppression (accept restrictions, reduce actual impact, maintain presence through compliance theater), while the field worker's snare perspective reflects the actual human cost of that adaptation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness chi derives from their structural position (power, exit options, beneficiary/victim status) and the base extractiveness ε. Field workers are victims with trapped exit → high d (≈0.95) → high f(d) (≈1.42) → high chi; they bear maximum extraction. Beneficiary populations are victims with trapped exit → high d → high chi; they experience service extraction. The host state is beneficiary with arbitrage exit → low d (≈0.05) → negative f(d) (≈-0.12) → negative chi; they experience the constraint as net benefit, not cost. Domestic NGO partners are victims + beneficiaries (mixed) with constrained exit → moderate d (≈0.50-0.60) → moderate f(d) (≈0.65-0.75) → moderate chi; they experience mixed extraction and benefit. International headquarters are institutional actors with constrained exit (can theoretically withdraw but costs are high) + beneficiary status (continue operations) → low-moderate d (≈0.25-0.35) → low f(d) (≈0.08-0.25) → low-moderate chi; they experience the constraint as bearable cost of operation. Analytical observer is purely observer → d≈0.72 → high f(d) → high chi for analytical perspective, but this is an artifact of the observation position (the observer bears no actual cost). The directionality pattern reveals that the constraint's extraction is highly asymmetric: concentrated on powerless field workers and trapped beneficiary populations, distributed across organizational levels for institutional actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that field workers and beneficiary populations experience genuine snares (high extraction, trapped exit, no coordination function), while the host state experiences genuine rope (coordination of civil society management), and international headquarters experience piton (degraded organizational function maintained through theater). The snare classification is not mislabeling coordination as extraction — there is no coordination benefit to field workers or beneficiaries from the regulatory accumulation. The rope classification is not mislabeling extraction as coordination — the state genuinely solves a coordination problem (managing civil society presence, controlling narratives) even while extracting. The piton classification correctly identifies that headquarters have abandoned the primary function (impact on beneficiary populations) in favor of operational persistence. The mandatrophy reveals that the constraint is a structural snare for trapped agents but appears as rope or piton from positions with exit options or adaptive capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_voluntary_state_ngo_relations,
    'Is the reduction in NGO operations a natural consequence of sovereignty assertion or a deliberate extraction mechanism designed to reduce civil society accountability?',
    'Comparative institutional analysis: countries with strong NGO sectors despite robust regulatory frameworks (Rwanda, Vietnam with selective enforcement) vs countries with severe NGO restrictions despite non-threatening sector (Egypt, Turkey). Pattern analysis of whether restrictions correlate with threatened elites or universal sovereignty claims.',
    'If natural sovereignty: mountain perspective is correct, and the constraint is universal. If deliberate extraction: snare classification is confirmed, and the constraint is contingent on specific state preferences. This resolves the false summit diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_vs_voluntary_state_ngo_relations, empirical, 'Whether operational reduction reflects sovereignty law or deliberate extraction').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.68) primarily structural (legal barriers, funding cuts, movement restrictions) or partially internalized (NGO self-censorship, anticipatory compliance, internalized state framing)?',
    'Post-reduction behavior tracking: do NGO staff maintain internalized compliance after leaving the country? Do organizations operating in diaspora maintain the same self-censorship? Comparison of stated concerns vs actual restrictions in legal code.',
    'If primarily internalized: the actual structural suppression is lower, but the effective suppression is higher because it persists in agents'' behavior after external barriers are removed. The snare classification is confirmed by internalization depth. If primarily structural: exit to diaspora reduces suppression significantly, changing experienced chi for expatriate actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    intermediate_organizational_survival_modes,
    'As restrictions increase, do NGOs transition through stable intermediate forms (localization, hybridization with state actors, diaspora shift) or do they face binary collapse-or-assimilation dynamics?',
    'Longitudinal organizational tracking: map NGO structural transitions as restrictions increase. Identify intermediate forms that maintain some function under constraints. Analyze whether these forms represent genuine adaptation or degradation toward piton status.',
    'If stable intermediates exist: tangled rope classification may be more appropriate for some organizations than snare; the constraint space is more structured. If binary dynamics: snare classification is confirmed; organizations cannot maintain genuine service provision under severe restrictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermediate_organizational_survival_modes, empirical, 'Existence of stable intermediate organizational forms').

omega_variable(
    aid_fungibility_extraction_component,
    'What portion of the host state''s benefit from NGO restrictions comes from captured resources (redirected aid, seized assets) vs political control (reduced accountability, constraint on advocacy)?',
    'Fiscal analysis of state budgets and aid flows; tracking of seized NGO assets and redirected funds. Comparative analysis of political openness in countries with high vs low NGO restrictions. Separate political control benefit from resource extraction benefit.',
    'If fungibility dominates: the extraction is primarily economic, making the constraint more classifiable as a snare with pure resource capture. If political control dominates: the extraction is informational/political, and the constraint may be better understood as a tangled rope where the state coordinates civil society control alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aid_fungibility_extraction_component, empirical, 'Proportion of extraction driven by resource capture vs political control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_ngo_operational_reduction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ngoreduction_tr_t0, international_ngo_operational_reduction, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ngoreduction_tr_t3, international_ngo_operational_reduction, theater_ratio, 3, 0.4).
narrative_ontology:measurement(ngoreduction_tr_t6, international_ngo_operational_reduction, theater_ratio, 6, 0.5).
narrative_ontology:measurement(ngoreduction_tr_t10, international_ngo_operational_reduction, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(ngoreduction_be_t0, international_ngo_operational_reduction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ngoreduction_be_t3, international_ngo_operational_reduction, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ngoreduction_be_t6, international_ngo_operational_reduction, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(ngoreduction_be_t10, international_ngo_operational_reduction, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_ngo_operational_reduction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(international_ngo_operational_reduction, 0.12).
narrative_ontology:affects_constraint(international_ngo_operational_reduction, international_aid_fungibility).
narrative_ontology:affects_constraint(international_ngo_operational_reduction, civil_society_advocacy_suppression).
narrative_ontology:affects_constraint(international_ngo_operational_reduction, state_accountability_reduction).

% DUAL FORMULATION NOTE:
% International NGO operational reduction is downstream of state capacity/sovereignty concerns but structurally distinct from general civil society regulation. The constraint operates through regulatory accumulation targeting international organizations specifically. Related constraints on domestic NGOs and domestic civic space are separate stories with different ε values and different beneficiary/victim profiles. These three stories form a civil society constraint family linked by regulatory spillover effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_ngo_operational_reduction, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
